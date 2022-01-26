# homework 1 code all code used

# Question 1 
# unzipping the HW1 tarball
tar -xvf HW1.tar.gz

# zipping files code
tar -czvf HW1.tar.gz HW1

# downloading basketball file 
wget https://kenpom.com/cbbga22.txt

# change permissions
# remove write permission and add execute for all users 
chmod a-w+x TB_microbiome_data.txt 

# removing read permission for all users
chmod a-r TB_microbiome_data.txt 

# Removing read access for group and other users
chmod go-r TB_microbiome_data.txt 

# Games played by SUU
grep -c "Southern Utah" cbbga22.txt

# inverse search count for games played other than SUU
grep -vc "Southern Utah" cbbga22.txt

# Viral.fasta file
# total genomes
wc -l viral.fasta 

# coronavirus genomes
grep -c "coronavirus" viral.fasta

# SARS genomes
grep -c "SARS" viral.fasta

# counting the number of A's in all the files
grep -c 'a' *.*
grep -c 'A' *.*

# getting species counts
grep -a "Staphylococcus" TB_microbiome_data.txt
grep -c "Streptococcus" TB_microbiome_data.txt
grep -c "Mycobacterium tuberculosis" TB_microbiome_data.txt

# Question 5
# using less
#wrap long lines (default), displays line numbers,
less -SNp "corona" viral.fasta 

#Question 6
# opening the text file
vim cbbga22.txt 

# moving to the beginning or end of the line
# moving to the start of the line
gg

## moving to the end of the line 
G

# insert
i

# copy a line
yy

# copy a word
yw

# paste whatever has been copied
p

# delete highlighted
d

# delete word
dw

# delete line
dd

#Save a file
:w

# exit vim
:wq






