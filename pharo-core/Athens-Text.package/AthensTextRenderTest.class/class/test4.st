as yet unclassified
test4
	|  t  c surf |

"t := '12345 pi kl mn op gj the text must flow' "

t := 'Unfortunately this class has not been documented yet.' 
asText .
	t addAttribute: (TextFontReference toFont: (
LogicalFont
 familyName: 'Tahoma'
 pointSize: 10
)).
	t addAttribute: (TextColor new color: Color red) from: 3 to: 10.


	c := AthensTextComposer scan: t for: AthensTextScanner new.

	surf := self surfaceClass extent: Display extent.
	
	surf drawDuring: [ :canvas | 
		
		canvas setPaint: (Color yellow alpha: 0.5).
		canvas drawShape: (0@0 corner: 300@300).

		canvas pathTransform scaleBy: 1.
		1 to: 20 do: [:i |
			c renderOn: canvas.
			canvas pathTransform translateBy: 0.1@10.
		]
	].
	Display getCanvas translucentImage: surf form at: 100@100

