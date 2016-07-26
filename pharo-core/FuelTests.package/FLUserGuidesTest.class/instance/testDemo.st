tests-Getting Started
testDemo

| sourceArray loadedArray |
sourceArray := 
	Array 
		with: 'a string' 
		with: Transcript
		with: [ Transcript show: 'a string'; flush ].
"Store to the file"
FLSerializer serialize: sourceArray toFileNamed: 'example.FL'. 
"Load from the file"
loadedArray := FLMaterializer materializeFromFileNamed: 'example.FL'. 
"The arrays are not the same"
[ sourceArray ~~ loadedArray ] assert.
"The strings are not the same"
[ sourceArray first ~~ loadedArray first ] assert.
[ sourceArray first = loadedArray first ] assert.
"The global instance Transcript is the same"
[ sourceArray second == loadedArray second ] assert.
"Look at Transcript how the loaded block prints a string"
loadedArray third value.