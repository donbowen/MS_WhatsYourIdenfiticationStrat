If you use this dataset, please cite "What's Your Identification Strategy? 
Innovation in Corporate Finance Research" by Donald Bowen, Laurent Fresard, 
and Jerome Taillard. Management Science, Forthcoming. Available at 
http://pubsonline.informs.org/doi/abs/10.1287/mnsc.2016.2437

Folder contents:

*in_data/bft_all_paper_obs.dta
	A stata file containing the data. Can be immediately used by the do	
	files to replicate the paper's results. Caveat: only covers results
	based on a paper level sample. So Tables 5-7, which use an author 
	career panel, are not included. Figure 3 is based on editor data
	at the journal year level.

*in_data/bft_all_paper_obs_WITHreadme.xlsx
	Two excel sheets.
	The first is a description of variables.
	The second is the same dataset, in Excel form.

*bft_figures.do
	Will reproduce Figures 1, 2, and 4.
	
*bft_tables.do
	Will reproduce Table 1-4.
	
*bft_functions.do
	Needed by bft_figures.do and bft_tables.do.