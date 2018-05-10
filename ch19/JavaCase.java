class JavaCase{
	public static void main(String[] args){
		String[] a1 = {"hello", "word"};
		Object[] a2 = a1;
		a2[0] = new Integer(17);
		String s = a1[0];
		
	}
}
