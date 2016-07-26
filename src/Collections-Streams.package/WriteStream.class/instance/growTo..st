private
growTo: anInteger
 
    " anInteger is the required minimal new size of the collection "
 	| oldSize length |
 	oldSize := collection size.
      length := anInteger + (oldSize // 4 max: 20) - oldSize.
 	collection := collection grownBy: length.
 	writeLimit := collection size.
 