*manifest-core
manifestReadOn: aArray

 	^ self className:  (aArray at: 1) 
		selector: (aArray at: 2)
		isMetaSide: ((aArray at: 3) = #true)   
		