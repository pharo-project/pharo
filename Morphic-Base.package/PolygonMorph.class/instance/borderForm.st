private
borderForm
	"A form must be created for drawing the border whenever the borderColor is translucent."

	| borderCanvas |
	borderForm ifNotNil: [^ borderForm].
	borderCanvas := (Display defaultCanvasClass extent: bounds extent depth: 1)
		asShadowDrawingCanvas: Color black.
	borderCanvas translateBy: bounds topLeft negated
		during:[:tempCanvas| self drawBorderOn: tempCanvas].
	borderForm := borderCanvas form.
	self arrowForms do:
		[:f |  "Eliminate overlap between line and arrowheads if transparent."
		borderForm copy: f boundingBox from: f to: f offset - self position rule: Form erase].
	^ borderForm