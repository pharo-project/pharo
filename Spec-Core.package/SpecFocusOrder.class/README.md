A SpecFocusOrder manage the path of focus between widgets.

I use a collection (presenters) to keep widgets I managed. When I reach the end of the collection, I try to give the focus at the parent in the hierarchy, if it is nil I loop.

I provide the following methods
- add: and addLast: to add a widget in my collection
- giveFocusToNextFrom:for: and giveFocusToPreviousFrom:for: to make me change the focus, the first argument is presenter who leave the focus and the second is the current model in the hierarchy.
- ifEmpty:ifNotEmpty: and ifNotEmpty: are send in my collection.
- presenters is the getter of my collection.
- removeAll to remove all my widgets.

My super class is not OrderedCollection because it could be a problem for the devellopement of another behaviour.