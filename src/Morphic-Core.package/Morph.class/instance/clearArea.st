Morphic-Base-Worlds
clearArea
	"Answer the clear area of the receiver. It means the area free  
	of docking bars."
	| visTop visBottom visLeft visRight |

	visTop := self top.
	visBottom := self bottom.
	visLeft := self left.
	visRight := self right.

	self dockingBars
		do: [:each | 
			(each isAdheringToTop and: [each bottom > visTop])
				ifTrue: [visTop := each bottom].

			(each isAdheringToBottom and: [each top < visBottom])
				ifTrue: [visBottom := each top].

			(each isAdheringToLeft and: [each right > visLeft])
				ifTrue: [visLeft := each right].

			(each isAdheringToRight and: [each left < visRight])
				ifTrue: [visRight := each left]
		].

	^ Rectangle
		left: visLeft
		right: visRight
		top: visTop
		bottom: visBottom
