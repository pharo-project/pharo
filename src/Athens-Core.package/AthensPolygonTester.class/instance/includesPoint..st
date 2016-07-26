testing
includesPoint: aPoint
"
Thanks to Google and Randolph Franklin i don't have to reinvent this very simple algorithm.
See [ 1 ] for details, copyrights etc.

[1] http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

"
	| inside testX testY i j size |
	
	inside := false.
	
	i := 1.
	size := polygon size.
	j := size.
	testX := aPoint x.
	testY := aPoint y.
	
	[ i <= size ] whileTrue: [  | pi pj |
		pi := polygon at: i.
		pj := polygon at: j. 
	
		(((pi y > testY) ~= (pj y > testY)) and: [ 
			testX < 
			(
				pj x - pi x
				* (testY - pi y) 
				/ ( pj y - pi y )
				+ pi x 
			)
			 ]) ifTrue: [ inside := inside not ].
	
		j := i.
		i := i + 1.
	 ].
	^ inside
	