comparing
= aRegistration
    aRegistration class == self class
        ifFalse: [ ^ false ].
    ^ (configurationProjectSpec registrationsCompareEqual: aRegistration configurationProjectSpec)
        and: [ baselineProjectSpec registrationsCompareEqual: aRegistration baselineProjectSpec ]