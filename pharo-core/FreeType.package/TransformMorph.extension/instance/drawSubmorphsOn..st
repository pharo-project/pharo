*FreeType-override
drawSubmorphsOn: aCanvas

	aCanvas transformBy: transform
		clippingTo: self innerBounds
		during: [:myCanvas |
			(self angle ~= 0.0 or: [self scale ~= 1.0])
				ifTrue:[ 
					FreeTypeSettings current forceNonSubPixelDuring:[
						submorphs reverseDo:[:m | myCanvas fullDrawMorph: m] ] ]
				ifFalse:[
					submorphs reverseDo:[:m | myCanvas fullDrawMorph: m] ] ] 
		smoothing: smoothing