object q1 {
    def main(args:Array[String]) = {

        val inventory1 = Map(
            101 -> ("Apple iPhone 13",10,799.99),
            102 -> ("Samsung Galaxy S21",5,999.99),
            103 -> ("Google Pixel 6",20,599.99),
            105 -> ("Sony",15,349.99),
            106 -> ("Apple Macbook Pro",7,1999.99),
            107 -> ("Dell XPS 13",12,1299.99),
            108 -> ("Amazon Echo Dot",25,49.99),
            109 -> ("Samsung Galaxy Tab S7",8,649.99)
        )

        val inventory2 = Map(
            102 -> ("Samsung Galaxy S21",3,979.99),
            104 -> ("OnePlus 9",7,729.99),
            105 -> ("Sony",10,339.99),
            110 -> ("Microsoft Surface Pro 7",6,899.99),
            111 -> ("Apple Watch Series 6",10,399.99),
            112 -> ("Fitbit Charge 4",15,149.99),
            113 -> ("Google Nest Hub",5,89.99)
        )

        //1
        printf("Products in the inventory 1:\n")
        inventory1.keys.foreach{i => println(inventory1(i)._1)}

        println()
        
        //2
        var Total1 = 0.0
        inventory1.keys.foreach(i => Total1 = Total1 + inventory1(i)._3)
        println(f"Total value of all products in inventory1 = Rs. $$${Total1}%.2f")

        println()

        //3
        if(!inventory1.isEmpty) {
            println("Inventory 1 is not empty")
        }
        else {
            println("Inevntory 1 is empty")
        }

        println()

        //4
        val mergedInventory = inventory1 ++ inventory2
        
        println("Two inventories after merging (productID -> (productName,quantity,price(Rs.))) :")
        mergedInventory.keys.toSeq.sorted.foreach{i => println(i + " -> " + mergedInventory(i))}

        println()

        //5
        if(inventory1.contains(102)) {
            println("Product ID 102 exists")
            println(inventory1(102))
        }
        else {
            println("Product ID 102 doesn't exist")
        }
    }
}
