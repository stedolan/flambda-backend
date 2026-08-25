let x = 1
[%%expect.type]

let f = function None -> 1
[%%expect.warn]
