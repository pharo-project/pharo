The layout process:
For computing the new layout for the children of any morph, we start with an initial rectangle which is provided as a reference.

Step 1: The first step of layout computation is to compute the minimum extent each of our children can have. The minimum extent is mapped through both the local layout frame of the morph (for relative positioning) and the global layout frame (for insets, such as cursor indication) to obtain the minimal size required for each cell.

Step 2: Based on the cell sizes, the number of cells we can put into each row and column is computed. For equal spacing, the maximum size of the cells is taken into account here.

Step 3: Based on the row/column sizes, we compute the extra space which should be added to each row/column. For 
	#leftFlush/#topFlush - we add all extra space add the end
	#rightFlush/#bottomFlush - we add all extra space at the start
	#centering - we add 1/2 of the extra space at start and end
	#justified - we distribute the space evenly between the morphs
[NOTE: If any #spaceFill morphs are encountered during this step, #justified is implied and the space is exclusively and equally distributed between those #spaceFill morphs. This is for backward compatibility and should *never* be necessary in the new regime].

Step 4: The morphs are placed in the computed cells and the extra space is distributed as necessary. Placing the submorphs is done by mapping through the global and the local layout frame as requested.

Start point:
=> bounds: new rectangle for the morph.

Compute basic arrangement of morphs:
=> For each submorph compute minExtent
	- if global layout frame inset in global layout frame
	- if local layout frame inset in local layout frame
=> Compute number of morphs per, width and height of row/column
	- if equal spacing based on max size
=> Compute extra space per row/column
	- if centering = #justified; distribute space equally
	- if centering #leftFlush/#topFlush (-1) add start extra
	- if centering #rightFlush/#bottomFlush (1) add end extra
	- if centering #centered add 1/2 extra to start/end
	<extra space must be float and rounded accordingly!>
=> Place morphs in appropriate cells
	- if global layout frame inset in global layout frame
	- if local layout frame inset in local layout frame
	<will likely cause #layoutChanged by submorphs>

Distribute morphs in row/column:

=> Compute the max length of each row/column
