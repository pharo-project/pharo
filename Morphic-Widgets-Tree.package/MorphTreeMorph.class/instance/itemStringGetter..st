accessing
itemStringGetter: aValuable
	"Set how to get a string for the first column with a valuable which 
	takes a row item (from a MorphTreeMorphNode point of view, its complexContents item) as argument"
	self columns first itemStringGetter: aValuable