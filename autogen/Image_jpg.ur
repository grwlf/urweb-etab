open Image_jpg_c
fun content {} = b <- blob () ; returnBlob b (blessMime "image/jpeg")
val propagated_urls : list url = 
    []
val url = url(content {})
val geturl = url
