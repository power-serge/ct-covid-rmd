echo "building county pages..."
Rscript BuildCountiesWebsite.R 

echo "uploading to github..."
d=$(date +"%Y_%m_%d")
git commit -m "Daily Update for $d"
git push




