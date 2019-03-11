#testing: to remove words starting with character
  
x <-  "@AgnezMo 0On 5AirAsia Airbus A320-216 Fleet 8with @NinetologyMY Livery -- 9M-AHG cc: @AgnesMonicaEnt @agnezone http://t.co/hfXwUQq2Oq"  

gsub("[0-9]\\w+ *", "", x)


va <- "fghhg gjgg .*Ahurottj dhfkkf. ahdhjf sdvd"

vb <- library(stringr)
str_extract(string = va, pattern = perl("(?<=.).*(?=.)"))



v1<-"fghhg gjgg .*Ahurottj dhfkkf. ahskdg ajsbd"
check<-strsplit(va,split='*', fixed=TRUE)[[1]][2]
check<-strsplit(va,split='*', fixed=TRUE)

library(qdapRegex)
rm_between(v1, ".", ".", extract=TRUE)


#bullet points


require(stringr)

string <- "passarão a estar inscritas políticas públicas que permitam:\n • Inverter a tendência de perda de 
rendimento das famílias, dos trabalhadores, dos\n funcionários públicos e dos pensionistas;\n"
#Split by \n

#  match semi-colon or colon, then a backslash, then "n". I.E. split by ;\n or :\n
stringList <- unlist(str_split(string, "([;:.])\\\n"))
#Return position of any string that starts with a bullet:

matched <- grep("\\\u0095", stringList)
matched <- grep("\\•", stringList)
#Subset to strings that start with bullets:

stringList[matched]
ignore
#  match semi-colon or colon, then a backslash, then "n". I.E. split by ;\n or :\n
stringList <- unlist(str_split(string, "([;:])\\\n"))
stringList1 <- unlist(str_split(string, "([.])\\\n"))
stringList1 <- unlist(str_split(string, "([.])"))
#Return position of any string that starts with a bullet:

matched <- grep("\\\u0095", stringList)
matched <- grep("\\•", stringList)
#Subset to strings that start with bullets:


test <- "15607 Hydraulic Excavators <b>Specifications:</b><br /><br /><ul><li>Engine Power : SAE J1349, net 184 kW (247 HP) @ 1,750 rpm</li><li>Operational Weight : 34,100 kg (75,178 lb) - STD.</li><li>Bucket capacity (SAE) : 1.25~1.83 m<sup>3</sup> (1.64 ~ 2.39 cu.yd)</li></ul> As per requirement As per requirement "
#case1
rm_between(test, "<ul>", "</ul>", extract=TRUE)

#case2

split_data <- unlist(str_split(test,"([<ul>]\\<)"))

data_match <- grep("\\li>",split_data)

split_data[data_match]

#making u udn for the same
my_function<-function(x)
{
  test1<-rm_between(x, "<ul>", "</ul>", extract=TRUE)
  test1<-test1[[1]]
  split_data <- unlist(str_split(test1,"([<ul>]\\<)"))
  my_str<-""
  for(i in 1:length(split_data))
  {
    my_str<-paste0(my_str," ",split_data[i])
  }
  return(my_str)
}


my_function(test1)

test1 <- "<p>Maximum tire life <br />The steering system is designed to maintain zero convergence / divergence when the tie rods and front suspension cylinders are properly adjusted to minimize tire tread and maximize tire life.</p><p>Suspension system <br />It is designed to dissipate the impacts of the haul road and load in order to prolong the life of the frame and provide a more comfortable ride.</p><p>Cylinders</p><p>It has four independent and autonomous cylinders of pneumatic suspension of nitrogen / oil and variable damping designed to absorb the impacts in the most rigorous applications.</p><p>Durable design <br />Heavy-duty cylinders that use a large bore design and low pressure nitrogen / oil to maximize durability with minimal maintenance.</p><p>Front <br />The front cylinders with axle tilt and inclination of the preset wheels are mounted on the frame and serve as steering pivots to perform closed turning radii with excellent maneuverability and low maintenance. <br />Rear The rear cylinders allow the axle to oscillate and absorb bending and torsional stresses caused by uneven and uneven haul roads instead of transmitting them to the main frame. <br /><br /> Damping <br />control Integrated damping control and cab strategically located in relation to the front axle that minimize the effect of pitching and bouncing, resulting in a more comfortable ride, greater productivity and less fatigue.</p>
"


df <- read.csv("E:/Product Classification_new/Motor Grader/Non Brand/non_brand_vlookUp_motor_grader")
write.csv(df,"file:///E:/Product Classification_new/Motor Grader/Non Brand/non_brand_vlookUp_motor_grader.csv",row.names = F,quote = F)
