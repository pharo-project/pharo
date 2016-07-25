private
loadFrom: aSerializeFile 
	^ self new
		oldRelationGraph: (FLMaterializer materializationFromFileNamed: aSerializeFile).