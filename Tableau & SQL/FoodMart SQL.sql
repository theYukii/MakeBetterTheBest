USE foodmart;

# A1
SELECT store_id, SUM(sales_fact.unit_sales) AS  Quantity, SUM(sales_fact.store_sales) AS Total_Sales
FROM product, sales_fact
WHERE product.product_id = sales_fact.product_id
AND product.product_name LIKE '%Potato Chips'
GROUP BY store_id
ORDER BY Total_Sales DESC;

# A2
SELECT the_month, SUM(sales_fact.store_sales) AS Total_Sales
FROM sales_fact, time_by_day
WHERE sales_fact.time_id = time_by_day.time_id
GROUP BY the_month
ORDER BY str_to_date(the_month,'%M');

# A3
SELECT S.store_id, sales_region, SUM(SF.store_sales) AS Total_Sales
FROM sales_fact SF, store S, region R
WHERE SF.store_id = S.store_id
AND S.region_id = R.region_id
GROUP BY S.store_id
ORDER BY Total_Sales DESC;

# A4
SELECT PC.product_category, SUM(SF.store_sales) AS Total_Sales
FROM product_class PC, product P, sales_fact SF
WHERE PC.product_class_id = P.product_class_id
AND P.product_id = SF.product_id
GROUP BY PC.product_category
ORDER BY Total_Sales DESC
LIMIT 1;

# A5
SELECT DAYNAME(T.the_date) AS Day_of_Week, SUM(SF.store_sales) AS Total_Sales
FROM time_by_day T, sales_fact SF
WHERE T.time_id = SF.time_id
GROUP BY Day_of_Week
ORDER BY Total_Sales DESC;