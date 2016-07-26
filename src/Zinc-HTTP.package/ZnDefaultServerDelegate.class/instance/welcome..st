responses
welcome: request
	"Answer a nice HTML welcome page"
	
	| page entity |
	page := self generateWelcomePage.
	entity := ZnEntity html: page. 
	^ ZnResponse ok: entity