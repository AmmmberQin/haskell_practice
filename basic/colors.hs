let color = ["black", "white", "blue", "yellow", "red"]
[(a,b) | a <- color, b <- color, a < b]