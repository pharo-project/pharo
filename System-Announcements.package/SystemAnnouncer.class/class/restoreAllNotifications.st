initialization
restoreAllNotifications
	"self restoreAllNotifications"
	
	| collector |
	self reset.
	collector := PragmaCollector filter: [ :pragma | pragma keyword = #systemEventRegistration ].
	collector reset.
	collector do: [ :pragma |
		pragma methodClass theNonMetaClass perform: pragma methodSelector ]