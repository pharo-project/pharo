private
setFont 
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	font installOn: bitBlt foregroundColor: foregroundColor backgroundColor: Color transparent