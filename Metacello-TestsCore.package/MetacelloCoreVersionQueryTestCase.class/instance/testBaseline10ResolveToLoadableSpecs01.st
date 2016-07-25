tests
testBaseline10ResolveToLoadableSpecs01
    | project version expected list |
    project := self project.
    version := project version: '1.0-baseline'.
    expected := #('UTF8').
    list := version resolveToLoadableSpecs: #('UTF8').
    self assert: list size = expected size.
    list do: [ :pkg | self assert: (expected includes: pkg name) ].
    expected := #('UTF8').
    list := version resolveToLoadableSpecs: #('Recursive').
    self assert: list size = expected size.
    list do: [ :pkg | self assert: (expected includes: pkg name) ]