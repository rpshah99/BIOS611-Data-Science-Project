docker build . -t project
docker run -p 8787:8787 -e USERID=$(id -u) -e GROUPID=$(id -g)  
-v $(pwd):/home/rstudio/work -it amoselb/rstudio-m1
docker run -e PASSWORD=project611 -p 8787:8787 amoselb/rstudio-m1