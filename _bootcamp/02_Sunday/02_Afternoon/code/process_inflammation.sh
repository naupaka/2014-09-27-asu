# bash script to process inflammation data
# call with "http://npk.io/RBzU+"
 
# echo Clean previous runs
# rm -r data*
 
echo Downloading data
cd ..
curl -L $1 -o downloaded_data.zip
 
sleep 2
 
echo Unzipping and then deleting zip file
unzip -x downloaded_data.zip
rm downloaded_data.zip
 
sleep 2
 
echo Available files
cd data
ls -a -F
 
sleep 2
 
echo Cleanup
rm ._*.csv
 
mkdir bad_data
mv *broken* *metadata* bad_data/
 
sleep 2
 
echo Renaming
todays_date=`date +%Y-%m-%d`

for f in *.csv
do
    mv $f ${f/-/_${todays_date}_}
done
 
echo Files after renaming
ls *csv
 
sleep 2
 
echo Processing
for f in *.csv
do
    echo Processing $f
    Rscript ../code/plot_inflammation.R $f # Mac and Linux version
    # /c/Program\ Files/R/R-3.1.0/bin/Rscript.exe plot_inflammation.R $f #windows version
    sleep 1
done
     
echo Move figures
cd ..
mkdir -p output/figures # -p switch creates intermediate directories as needed
mv -v data/*png output/figures/ # verbose move

echo Completed
