"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestFreeTypeGraphics',
	#superclass : 'PackageManifest',
	#category : 'FreeType-Graphics-Manifest',
	#package : 'FreeType-Graphics',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestFreeTypeGraphics class >> ruleLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FreeTypeGlyphRenderer #convert8to32:colorValue: #false)) #'2022-09-02T00:19:52.25468+02:00') #(#(#RGMethodDefinition #(#FreeTypeGlyphRenderer #renderGlyph:depth:subpixelPosition:font: #false)) #'2022-09-02T00:21:50.487906+02:00') )
]

{ #category : 'code-critics' }
ManifestFreeTypeGraphics class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#FreeTypeGlyphRenderer #convert8to32:colorValue: #false)) #'2022-09-02T00:19:55.654969+02:00') #(#(#RGMethodDefinition #(#FreeTypeGlyphRenderer #fixBytesForMono: #false)) #'2022-09-02T00:20:44.959596+02:00') #(#(#RGMethodDefinition #(#FreeTypeFont #displayLineGlyphOn:from:to: #false)) #'2022-09-02T00:22:10.387974+02:00') )
]
