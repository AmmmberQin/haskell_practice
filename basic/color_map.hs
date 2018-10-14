let colors = ["red", "green", "blue"]

[(tennessee, mississippi, alabama, georgia, florida) | tennessee <- colors, mississippi <- colors, alabama <- colors, georgia <- colors, florida <- colors, tennessee /= mississippi, tennessee /= alabama, tennessee /= georgia, mississippi /= alabama, georgia /= alabama, alabama /= florida, georgia /= florida]