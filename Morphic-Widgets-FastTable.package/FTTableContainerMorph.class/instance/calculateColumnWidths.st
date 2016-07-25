private
calculateColumnWidths
	"do three runs 
	- first collect defined columnwidth that fit
	- collect remaining undefined columnwidth 
	- return if all fit 
	  or collect and distribute remaining width"
	| undefinedColumnWidths widths remainingWidth |

	remainingWidth := self owner bounds width.

	widths := self owner columns
		collect: [ :c | | columnWidth | 
			columnWidth := c acquireWidth: remainingWidth. 
			remainingWidth := remainingWidth - columnWidth. 
			columnWidth ].
   "all fit - finish"		
	undefinedColumnWidths := widths count: #isZero.
   undefinedColumnWidths isZero ifTrue:[^ widths].

   "collect and distribute remaining space"
	^ widths collect: [ :c | 
		c = 0
			ifTrue: [ remainingWidth / undefinedColumnWidths]
			ifFalse: [ c ] ]