as yet unclassified
test5
	|  t  c surf |

"t := '12345 pi kl mn op gj the text must flow' "

t := 'U' 
asText .
	t addAttribute: (TextFontReference toFont: (
LogicalFont
 familyName: 'Tahoma'
 pointSize: 10
)).
	c := AthensTextComposer scan: t for: AthensTextScanner new.

	surf := self surfaceClass extent: Display extent.
	surf drawDuring: [ :canvas | 
		
		canvas setPaint: (Color yellow alpha: 0.5).
		canvas drawShape: (0@0 corner: 300@300).

		canvas pathTransform scaleBy: 1.
		1 to: 20 do: [:i |
			c renderOn: canvas.
			canvas pathTransform translateBy: 10@0.1.
		]
	].
	Display getCanvas translucentImage: surf form at: 100@100

