private
instancesStep

	clusters do: [ :aCluster | aCluster registerIndexesOn: encoder ].
	clusters do: [ :aCluster | self clusterInstancesStepOf: aCluster ]