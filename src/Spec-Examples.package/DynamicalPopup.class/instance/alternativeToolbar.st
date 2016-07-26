initialization
alternativeToolbar
	"Instanciate the alternative toolbar"

	self instantiateModels: #(#toolbar #OkToolbar).

	"Configuring the alternative toolbar"
	self toolbar okAction: [ Transcript show: self textInput text ].

	"Set the alternative toolbar in the window"
	self window toolbar: self toolbar