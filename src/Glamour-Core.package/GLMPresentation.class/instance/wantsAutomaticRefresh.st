accessing
wantsAutomaticRefresh
	"Answer whether this presentations supports automatic refresh. Unlike stepping
	the refresh rate is not controlled by the presentation but by the browser containing
	the presentation. If a presentation wants to control its refresh rate it should
	rely on #wantsStep."
	^ wantsAutomaticRefresh ifNil: [ false ]