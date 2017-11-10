Class {
	#name : #GLMDashboardTest,
	#superclass : #TestCase,
	#category : #Glamour-Tests-Core
}

{ #category : #tests }
GLMDashboardTest >> compareLayout: aLayout with: anOtherLayout [
	"I am here because when I create this method there is no equals method in LayoutFrame. Only pointers equality."

	^ aLayout leftFraction = anOtherLayout leftFraction
		and:
			[ aLayout rightFraction = anOtherLayout rightFraction
				and:
					[ aLayout topFraction = anOtherLayout topFraction
						and:
							[ aLayout bottomOffset = anOtherLayout bottomOffset
								and:
									[ aLayout leftOffset = anOtherLayout leftOffset
										and:
											[ aLayout rightOffset = anOtherLayout rightOffset
												and:
													[ aLayout topOffset = anOtherLayout topOffset
														and: [ aLayout bottomOffset = anOtherLayout bottomOffset ] ] ] ] ] ] ]
]

{ #category : #tests }
GLMDashboardTest >> testDefaultLayouts [
	| layout |
	layout := GLMDashboard new.
	layout addPaneNamed: #one.
	layout addPaneNamed: #two.
	layout addPaneNamed: #three.
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #one) with: (0 @ 0 corner: 0.5 @ 0.5) asLayoutFrame).
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #two) with: (0.5 @ 0 corner: 1 @ 0.5) asLayoutFrame).
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #three) with: (0 @ 0.5 corner: 1 @ 1) asLayoutFrame)
]

{ #category : #tests }
GLMDashboardTest >> testPanes [
	| layout |
	layout := GLMDashboard new.
	layout addPaneNamed: #one.
	layout addPaneNamed: #two.
	layout addPaneNamed: #three.
	self assert: layout panes size equals: 3.
	#(#one #two #three) do: [ :aSymbol | self assert: (layout panes anySatisfy: [ :aGLMPane | aGLMPane name = aSymbol ]) ]
]

{ #category : #tests }
GLMDashboardTest >> testPanesAndLayouts [
	| layout |
	layout := GLMDashboard new.
	layout addPaneNamed: #one layout: (0 @ 0 corner: 0.5 @ 0.5) asLayoutFrame.
	layout addPaneNamed: #two layout: (0 @ 0.5 corner: 0.5 @ 1) asLayoutFrame.
	layout addPaneNamed: #three layout: (0.5 @ 0 corner: 1 @ 1) asLayoutFrame.
	layout addPaneNamed: #four.
	self assert: layout panes size equals: 4.
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #one) with: (0 @ 0 corner: 0.5 @ 0.5) asLayoutFrame).
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #two) with: (0 @ 0.5 corner: 0.5 @ 1) asLayoutFrame).
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #three) with: (0.5 @ 0 corner: 1 @ 1) asLayoutFrame)
]
