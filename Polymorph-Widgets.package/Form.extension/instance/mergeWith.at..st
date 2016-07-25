*Polymorph-Widgets-Themes
mergeWith: aForm at: aPoint
	| mergedForm |

	mergedForm := self deepCopy.
	mergedForm getCanvas 
		translucentImage: aForm 
		at: aPoint. 
		
	^ mergedForm 