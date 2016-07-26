handlers
handleOnDownGrade: onDownGradeBlock onUpgrade: onUpgradeBlock
    ^ onUpgradeBlock cull: self cull: self existingProjectRegistration cull: self newProjectRegistration