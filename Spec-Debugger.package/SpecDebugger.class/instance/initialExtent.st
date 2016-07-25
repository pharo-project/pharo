api
initialExtent 
	"This code was taken from the old debugger to get the same behavior. Fell free to refactor"

	RealEstateAgent standardWindowExtent y < 400 "a tiny screen" 
		ifTrue: [^ 700@500].
	
	^ [ | w h |
		w := Display width.
		h := Display height.
		(w - (w // 3)) @ (h - (h // 5)) ]
		on: Error
		do: [ 850@600 ]