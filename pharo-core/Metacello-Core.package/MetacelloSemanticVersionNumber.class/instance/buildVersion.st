accessing
buildVersion
    buildVersion ifNil: [ buildVersion := #() ].
    ^ buildVersion