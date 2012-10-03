class HelloWorld
		implements Runnable{

		public int foo;
		private char bar;
		public static int baz = 42;
		public static float boo = 3.14159f;

		public void setup(){
				foo = 5;
				bar = 'c';
		}

		public static void main (String args[]) {
				System.out.println("Hello World!\n");
		}

		public void run (){
		}

}
