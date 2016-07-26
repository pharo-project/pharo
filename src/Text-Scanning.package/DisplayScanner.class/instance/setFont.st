private
setFont 
	foregroundColor := self defaultTextColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	text ifNotNil:[destY := lineY + line baseline - font ascent]