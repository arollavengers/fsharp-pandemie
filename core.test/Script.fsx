let f x = x * x

type City = {NbCubes : int} 
// Atlanta|Miami|Washington|MexicoCity|Chicago

let Atlanta = {NbCubes=0}

let InfectCity city =
    {NbCubes = city.NbCubes + 1}

let NewAtlanta = InfectCity Atlanta

