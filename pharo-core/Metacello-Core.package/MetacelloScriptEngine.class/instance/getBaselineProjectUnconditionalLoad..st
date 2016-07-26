project lookup
getBaselineProjectUnconditionalLoad: unconditionalLoad
    | project |
    project := (self getBaselineUnconditionalLoad: unconditionalLoad) project.
    project version spec repositories: self repositories copy.
    ^ project