accessing
avoidVisibleBordersAtEdge: aBoolean 
	"Instruct the receiver to avoid showing the borders at edge"
	avoidVisibleBordersAtEdge := aBoolean.
self updateLayoutProperties.