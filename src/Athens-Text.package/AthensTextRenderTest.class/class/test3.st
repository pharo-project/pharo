as yet unclassified
test3
	|  t  c surf |

"t := '12345 pi kl mn op gj the text must flow' "

t := 'qW' 
asText .
	t addAttribute: (TextFontReference toFont: (
LogicalFont
 familyName: 'Arial'
 pointSize: 8
)).

	c := AthensTextComposer scan: t for: AthensTextScanner new.

	surf := self surfaceClass extent: Display extent.
	surf drawDuring: [ :canvas | 
		
		canvas setPaint: (Color yellow alpha: 0.5).
		canvas drawShape: (0@0 corner: 300@300).

		canvas pathTransform translateBy: 200@200.
				
"		canvas pathTransform rotateByDegrees: 90. "
		canvas pathTransform scaleBy: 4.
	1 to: 10 do: [:i |
		
		canvas setPaint: (Color blue alpha: 0.5).
		canvas drawShape: (0@0 corner: 100@10).
		
		c renderOn: canvas.
		
		canvas pathTransform rotateByDegrees: 5. 
		canvas pathTransform translateBy: 0 @11 .
	].
	].
	Display getCanvas translucentImage: surf form at: 100@100

