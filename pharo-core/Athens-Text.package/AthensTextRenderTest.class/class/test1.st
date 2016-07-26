as yet unclassified
test1
	|  t  c  |

t := '
AthensTextRenderTest test1

leftFlush
p := MultiNewParagraph new.

p compose: t
	style: TextStyle default
	from: 1
	in: (0@0 corner: 100@100).
p

MultiNewParagraph
TextStyle default alignment


globalBounds:  toLocal: 
' 
asText .

	t addAttribute: (TextColor new color: Color red) from: 3 to: 10.
	t addAttribute: (TextEmphasis italic) from: 5 to: 13.

	c := AthensTextComposer scan: t for: AthensTextScanner new.


	AthensSceneView new
	scene:  [:canvas |
		canvas pathTransform scaleBy: 0.8.
		c renderOn: canvas
	];
	openInWindow
	 
