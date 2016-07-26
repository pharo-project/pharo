tests
testStableVersionMethod
    | project version |
    project := self projectWith: #(#'platformVersion1.x').
    self assert: (project version: #'stable') = project stableVersion