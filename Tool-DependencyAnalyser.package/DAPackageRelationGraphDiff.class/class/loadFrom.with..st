private
loadFrom: aSerializeFile with: newRelationGraph
	^ self new
		oldRelationGraph: (FLMaterializer materializationFromFileNamed: aSerializeFile);
		newRelationGraph: newRelationGraph