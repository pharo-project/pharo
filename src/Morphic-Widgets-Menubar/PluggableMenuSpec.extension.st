Extension { #name : #PluggableMenuSpec }

{ #category : #'*Morphic-Widgets-Menubar' }
PluggableMenuSpec >> asMenubarMenuMorph [
	^ self asMenuMorphOfKind: MenubarMenuMorph
]
