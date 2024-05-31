precio_articulo::String -> Integer
precio_articulo art
	|(art == "pinza") = 75
	|(art == "tenaza") = 20
	|(art == "brocha") = 80
	|(art == "martillo") = 30
	|(art == "yunque") = 40
	|otherwise = 0
	
	
precio_pedido::[String] -> Integer
precio_pedido [] = 0
precio_pedido (x:xs) = precio_articulo x + precio_pedido xs 


generar_listabis::[Integer] -> Integer -> [Integer]
generar_listabis lista p = [ x | x <- lista, x > p]

generar_lista::[Integer] -> Integer -> [Integer]
generar_lista [] _ = []
generar_lista (x:xs) p = if x >= p then [x] ++ generar_lista xs p else generar_lista xs p