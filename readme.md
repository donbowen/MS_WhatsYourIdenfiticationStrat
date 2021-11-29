# Data and Replication Kit for  "What's Your Identification Strategy? Innovation in Corporate Finance Research"

If you use this dataset, please cite "What's Your Identification Strategy? Innovation in Corporate Finance Research" by [Donald Bowen](https://bowen.finance), [Laurent Fresard](https://people.lu.usi.ch/fresal/), 
and Jerome Taillard. Management Science, 2017. 63(8):2529-2548. Available at http://pubsonline.informs.org/doi/abs/10.1287/mnsc.2016.2437

Folder contents:

- _in_data/bft_all_paper_obs.dta_
	A stata file containing the data. Can be immediately used by the do	
	files to replicate the paper's results. Caveat: only covers results
	based on a paper level sample. So Tables 5-7, which use an author 
	career panel, are not included. Figure 3 is based on editor data
	at the journal year level.
- _in_data/bft_all_paper_obs_WITHreadme.xlsx_ Two excel sheets. The first is a description of variables. 	The second is the same dataset, in Excel form.
- _bft_figures.do_ 	Will reproduce Figures 1, 2, and 4.
- _bft_tables.do_ Will reproduce Tables 1-4.
- _bft_functions.do_ 	Needed by bft_figures.do and bft_tables.do.

