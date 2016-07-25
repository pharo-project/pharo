responses
favicon: request
	"Answer a nice favicon for browsers to display"

	| entity |
	entity := ZnEntity with: ZnConstants faviconBytes type: 'image/vnd.microsoft.icon'. 
	^ ZnResponse ok: entity