accessing
projectWith: projectAttributes
    | project |
    "Construct Metacello project"
    project := MetacelloProject new.
    project projectAttributes: projectAttributes.
    MetacelloVersionConstructor on: self project: project.
    ^ project