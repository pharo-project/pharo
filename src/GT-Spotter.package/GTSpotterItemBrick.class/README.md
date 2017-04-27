I am a visual element that represents a single search result item within a category and is located in the list of all result items in Spotter.

I consist of icon, label and action bar with item related actions. Generally, I look like:

[ .... == Icon ==  .........    ==Label==   ........................ == Action bar == ..... ] >

I can be hovered with the mouse and selected by navigating with the help of keyboard arrows or click-selected with mouse.
Once hovered or selected my action buttons become visible.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	actionbarBrick:		<GLMBrick> - is a container of action buttons
	iconBrick:		<GLMBrick> - represents a search result icon. May be empty / invisible, still occupies space
	labelBrick:		<GLMBrick> - shows a textual representation of a search result item
	candidateLink:		<GTSpotterCandidateLink> - a link to the search result candidate. Link know its successor and predecessor within a result list
	stepModel:		<GTSpotterStep> - reference to the spotter step context that produced a candidate item I visually represent


    Implementation Points