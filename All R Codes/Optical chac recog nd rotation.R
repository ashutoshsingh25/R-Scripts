# install.packages("tesseract")
# install.packages("magick")
# install.packages("pdftools")


library(tesseract)
library(magick)
library(pdftools)

magick_config()

ls('package:tesseract')
ls('package:magick')
?ocr
?image_rotate
?image_trim
?tesseract_info

Text <- ocr("c")
Text
getwd()
Text2 <- ocr("https://5.imimg.com/data5/WB/OO/MY-14113623/aceclofenac-100mg-paracetamol-tablet-325mg-serratiopeptidase-15mg-250x250.jpg")
Text2

imj <- image_read("C://Users//imart/Downloads/iaccleondex-500x500.jpg")
Text2c <- image_ocr(imj)
Text2c

image_rotate(imj,15)
image_flop(imj) 
c(image_flop(imj),image_flip(imj),image_rotate(imj,15),image_rotate(imj,35))
Text2d <- ocr_data("https://5.imimg.com/data5/WB/OO/MY-14113623/aceclofenac-100mg-paracetamol-tablet-325mg-serratiopeptidase-15mg-250x250.jpg")
Text2d

text3 <-  ocr("https://3.imimg.com/data3/OE/YO/MY-9157849/paracetamol-500x500.jpg")
cat(text3)

text4 <- ocr("https://5.imimg.com/data5/KB/TP/MY-639811/indiamart-carewell-file-carnim-p-250x250.png")
text4

text5 <- ocr("https://5.imimg.com/data5/IC/XJ/MY-947603/1-250x250.jpg")
text5

text6 <- ocr("https://5.imimg.com/data5/YW/NN/MY-35997839/dynamic-e-commerce-website-services-250x250.jpg")
text6

text7 <- ocr("https://5.imimg.com/data5/GI/BY/MY-42845356/dynamic-website-designing-service-500x500.jpg")
text7

text8 <- ocr("https://5.imimg.com/data5/MD/FO/MY-41222635/web-designing-service-250x250.jpg")
text8


text9 <- ocr("https://5.imimg.com/data5/GI/BY/MY-42845356/dynamic-website-designing-service-500x500.jpg")

text9

text10 <- ocr("https://4.imimg.com/data4/YF/OC/MY-2328474/abiraterone-acetate-tablets-500x500.jpg")

text10

text11 <- ocr("https://5.imimg.com/data5/EY/SQ/MY-43954704/arthritis-drugs-250x250.jpg")
text11

test12 <- ocr("https://5.imimg.com/data5/LD/ES/MY-441649/piles-capsules-500x500.jpg")
test12

test13 <- ocr("https://3.imimg.com/data3/OD/AF/MY-2585742/best-piles-management-250x250.jpg")
test13
