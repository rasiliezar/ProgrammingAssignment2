## Tarea 2 de R Programming
## Ejecuta una sola vez la creación de una matriz inversa

## Definir sets y gets para las funciones

makeCacheMatrix <- function(x = matrix()) {
	
	miv <- NULL

	set <- function(y) {
      	x <<- y
        	miv <<- NULL
    	}
    
    	get <- function() x

    	setmiv <- function(inverse) miv <<- inverse
    	
    	getmiv <- function() miv

    	list(set = set, get = get, setmiv = setmiv, getmiv = getmiv)
}

## Regresa una matriz que es la inversa de x

cacheSolve <- function(x, ...) {
    	miv <- x$getmiv()

     	if (!is.null(miv)) {
        	message("Obteniendo data de caché")
        	return(miv)
    	}

    	data <- x$get()
    	miv <- solve(data, ...)

    	x$setmiv(miv)

    	miv
}