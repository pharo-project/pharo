tests
testBaseline10Groups
    | project version expected list |
    project := self project.
    version := project version: '1.0-baseline'.
    expected := #('Core' 'Tests' 'Recursive').
    list := version groups.
    self assert: list size = expected size.
    list do: [ :pkg | self assert: (expected includes: pkg name) ]