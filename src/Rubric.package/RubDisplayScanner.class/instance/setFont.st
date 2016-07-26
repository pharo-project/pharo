stop conditions
setFont 
	foregroundColor := self defaultTextColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	font installOn: bitBlt foregroundColor: foregroundColor backgroundColor: Color transparent.
	text ifNotNil:[
		baselineY := lineY + line baseline.
		destY := baselineY - font ascent].
