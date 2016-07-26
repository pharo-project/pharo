as yet unclassified
setBaselineRepositoryDescription: aListOrRepositoryDescriptions
    "set #version repositories to < aListOrRepositoryDescriptions>. Should be the directory where the BaselineOf is located."

    aListOrRepositoryDescriptions do: [:desc | self version spec repository: desc]