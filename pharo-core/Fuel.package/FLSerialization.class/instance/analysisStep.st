private
analysisStep

	| anAnalysis |
	anAnalysis := analyzer analysisFor: root.
	clusters := anAnalysis clusterization clusters.
	encoder objectCount: anAnalysis clusterization objectCount
