tests
testBaseline10Projects
    | project version expected list |
    project := self project.
    version := project version: '1.0-baseline'.
    expected := #('UTF8').
    list := version projects.
    self assert: list size = expected size.
    list do: [ :pkg | self assert: (expected includes: pkg name) ]