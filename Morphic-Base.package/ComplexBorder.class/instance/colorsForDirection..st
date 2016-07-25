private
colorsForDirection: direction 
	"Return an array of colors describing the receiver in the given direction"

	| colorArray dT cc |
	cc := self colors.
	direction x * direction y <= 0 
		ifTrue: 
			["within up->right or down->left transition; no color blend needed"

			colorArray := (direction x > 0 or: [direction y < 0]) 
						ifTrue: 
							["up->right"
							cc copyFrom: 1 to: width]
						ifFalse: 
							["down->left"
							"colors are stored in reverse direction when following a line"
							(cc copyFrom: width + 1 to: cc size) reversed]]
		ifFalse: 
			["right->down or left->up transition; need color blend"

			colorArray := Array new: width.
			dT := direction x asFloat / (direction x + direction y).
			(direction x > 0 or: [direction y >= 0]) 
				ifTrue: 
					["top-right"

					1 to: width
						do: 
							[:i | 
							colorArray at: i put: ((cc at: i) mixed: dT with: (cc at: cc size - i + 1))]]
				ifFalse: 
					["bottom-left"

					1 to: width
						do: 
							[:i | 
							colorArray at: i put: ((cc at: cc size - i + 1) mixed: dT with: (cc at: i))]]].
	^colorArray