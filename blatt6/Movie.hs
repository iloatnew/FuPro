module Movie where

-- 28.7.2017

 import System.Process
 import System.Directory

 infixl 6 &
 
 (&) :: String -> String -> String
 x & val = x ++ "=\"" ++ val ++ "\""

 type Pos = (Int,Int)
 
 mkFile dir i | i < 10  = dir ++ "/00" ++ file
	      | i < 100 = dir ++ "/0"  ++ file
	      | True    = dir ++ '/'   :  file
	      		  where file = show i ++ ".svg"

 isPic :: String -> Bool
 isPic file = lg > 4 && 
              drop (lg-4) file `elem` words ".eps .gif .jpg .pdf .png .svg"
	      where lg = length file

 movie :: String -> Bool -> IO ()
 movie dir frame = do files <- getDirectoryContents dir
                      html frame dir [dir++'/':file | file <- files, isPic file]

 movies :: String -> Bool -> [String] -> [Pos] -> IO ()
 movies file frame dirs ps = do files <- mapM getDirectoryContents dirs
                                let picFiles i = filter isPic $ files!!i
			            getFile (i,j) = dirs!!i++'/':picFiles i!!j
                                html frame file $ map getFile ps

 movieSelect dir = movies dir False [dir] . map (\i -> (0,i))

 html :: Bool -> String -> [String] -> IO ()
 html frame dir files@(first:rest) = writeFile (dir ++ ".html") $
    "<html>\n<head>\n<title>" ++ dir ++ 
    "</title>\n<script type"&"text/javascript" ++ 
    ">\nvar images = new Array(" ++ '\"':first ++ '\"':concatMap f rest ++ 
    ")\n</script>\n<script type"&"text/javascript" ++ " src"&"Painter.js" ++ 
    ">\n</script>\n</head>\n<body style"&"background-color: rgb(221,221,255)" ++
    ">\n<input type"&"button" ++ " value"&"|<<" ++ " onclick"&"backStop(this)"++ 
    ">\n<input type"&"button" ++ " value"&"<" ++ " onclick"&"back()" ++ "> " ++ 
    show n ++ " file" ++ (if n == 1 then "" else "s") ++
    "\n<input type"&"button" ++ " value"&">" ++ " onclick"&"forth()" ++ 
    ">\n<input type"&"button" ++ " value"&">>|" ++" onclick"&"forthStop(this)"++ 
    ">\n<input type"&"button" ++ " value"&"loop" ++ " onclick"&"loop(this)" ++ 
    ">\n<br><input id"&"fileSlider" ++ " type"&"range" ++ " min"&"0" ++ 
    " max"&show (n-1) ++ " value"&"0" ++ " onchange"&"showFile(this.value)" ++ 
    ">\n<span id"&"file" ++ '>':first ++ "</span>\n<br>\n<input type"&"range" ++
    " min"&"0" ++ " max"&"2000" ++ " value"&"30" ++ 
    " onchange"&"showTime(this.value)" ++ ">\n<span id"&"time" ++ 
    ">30</span> millisecs\n<br><br><br>\n<" ++ open ++ " id"&"img" ++ 
    " src"&first ++ " width"&width ++ " height"&height ++ close ++ 
    ">\n</body>\n</html>"
  where n = length files
        f file = ",\"" ++ file ++ "\""
	(open,width,height,close) = 
	 if frame then ("iframe","2400","900"," frameborder"&"0"++">\n</iframe")
		  else ("img","600","600",">\n</img")
 html _ _ _ = return ()
 
 mkPdf file = system $ "pstopdf " ++ file

 mkPng :: String -> String -> IO ()
 mkPng dir file = do rawSystem "convert" [file',"-trim",new]
                     removeFile file'
		  where file' = dir ++ '/':file
		        new = take (length file'-4) file' ++ ".png"

-- mkPng works for eps files. For converting svg files to png use
-- https://cloudconvert.org/svg-to-png.				

 pngmovie :: String -> IO ()
 pngmovie dir = do files <- getDirectoryContents dir
	           mapM_ (mkPng dir) $ filter isPic files
                   files <- getDirectoryContents dir
	           html True dir [dir++'/':file | file <- files, isPic file]
	           
 getFiles :: String -> IO ()
 getFiles dir = do files <- getDirectoryContents dir; mapM_ putStrLn files

