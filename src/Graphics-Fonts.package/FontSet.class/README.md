FontSet provides a mechanism for storing a set of fonts as a class that can be conveniently filedOut, filedIn, and installed as a TextStyle.

The most common use is...
	Find a font you like.
	Use BitFont to convert a bunch of sizes to data files named, eg, LovelyNN.BF
	Use FontSet convertFontsNamed: 'Lovely' to produce a FontSet named Lovely.
	FileOut that FontSet for later use.
	Use Lovely installAsTextStyle to make all sizes available in a TextStyle
		named #Lovely in the TextConstants dictionary.
	Use ctrl-k in any text pane to select the new Lovely style for that paragraph.
	Then use cmd-1 through 5 or cmd-k to set the point-size for any selection.
