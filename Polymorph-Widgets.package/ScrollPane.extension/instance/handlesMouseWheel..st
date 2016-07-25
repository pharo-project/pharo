*Polymorph-Widgets
handlesMouseWheel: evt
	"Do I want to receive mouseWheel events?" 

	^self vIsScrollable or: [ self hIsScrollable ]